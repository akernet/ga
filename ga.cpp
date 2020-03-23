#include <iostream>
#include <vector>
#include <bits/stdc++.h>
#include <stdlib.h>

#include "lib/ga.hpp"

class Video {
    public:
        int size;
};

class EndpointCacheConnection {
    public:
        int cache_id, latency, endpoint_id;
};

class Cache {
    public:
        vector<EndpointCacheConnection> connections;

        // video id -> list of endpoints
        // used to avoid looping over all connected cache servers
        umap<int, vector<pair<int, int>>> potential_videos;
};

class Endpoint {
    public:
        int latency;
        vector<EndpointCacheConnection> connections;
};

class Request {
    public:
        int video_id, endpoint_id, number_of_requests;
};

class VideoCachePair {
    public:
        int v;
        int c;
};

vector<Video> videos;
vector<Cache> caches;
vector<Endpoint> endpoints;
vector<Request> requests;
int V, E, R, C, X;

template<>
Chromosome<VideoCachePair>::Chromosome(int number_of_genes) {
    for (int i = 0; i < number_of_genes; i++) {
        VideoCachePair vcp = VideoCachePair();

        // Generate random video-cache-pair
        vcp.c = rand() % (int) caches.size();
        vcp.v = rand() % (int) videos.size();

        genes.push_back(vcp);
    }
}

template<>
Chromosome<VideoCachePair>::Chromosome(vector<VideoCachePair> c, int number_of_genes) {
    genes = c;
    while (genes.size() < number_of_genes) {
        VideoCachePair vcp = VideoCachePair();

        // Generate random video-cache-pair
        vcp.c = rand() % (int) caches.size();
        vcp.v = rand() % (int) videos.size();

        genes.push_back(vcp);
    }
}

template<>
int Chromosome<VideoCachePair>::evaluate() {
    if (score != -1) {
        return score;
    }

    vector<int> space_left(caches.size(), X);

    // endpoint, video
    umap<pair<int, int>, int, hash_pair> lowest_latency;

    for (VideoCachePair & vc : genes) {
        // check space and add
        if (space_left[vc.c] - videos[vc.v].size >= 0) {
            space_left[vc.c] -= videos[vc.v].size;

            // check if video is requested for cache at all
            if (caches[vc.c].potential_videos.find(vc.v) == caches[vc.c].potential_videos.end()) {
                continue;
            }

            int len = caches[vc.c].potential_videos[vc.v].size();
            for (int j = 0; j < len; j++) {
                pair<int, int> p = caches[vc.c].potential_videos[vc.v][j];
                int endpoint_id = p.first;
                int latency = p.second;

                pair<int, int> ep = make_pair(endpoint_id, vc.v);

                if (lowest_latency.find(ep) == lowest_latency.end()) {
                    lowest_latency[ep] = latency;
                } else {
                    lowest_latency[ep] = min(lowest_latency[ep], latency);
                }
            }
        }
    }

    long long time_saved = 0;
    long long number_of_users = 0;
    for (Request & r : requests) {
        number_of_users += r.number_of_requests;

        pair<int, int> ev = make_pair(r.endpoint_id, r.video_id);
        if (lowest_latency.find(ev) == lowest_latency.end()) {
            continue;
        }
        int Ld = endpoints[r.endpoint_id].latency;
        time_saved += r.number_of_requests*max(0, (Ld - lowest_latency[ev]));
    }

    score = 1000*time_saved/number_of_users;
    return score;
}

template<>
void Chromosome<VideoCachePair>::mutate() {
    int n = genes.size();
    int cs = caches.size();
    int vs = videos.size();
    for (int i = 0; i < n; i++) {
        if (rn() % n == 0) {
            // randomly change cache or video
            if (rn() % 2 == 0) {
                genes[i].c = rn() % cs;
            } else {
                genes[i].v = rn() % vs;
            }

            // invalidate score
            score = -1;
        }
    }
}

int min_video_size = INT_MAX;

long long total_video_size = 0;
long long total_cache_size = 0;

void parse_data(fstream &io) {
    io >> V >> E >> R >> C >> X;

    total_cache_size = C*X;

    // cache servers
    for (int i = 0; i < C; i++) {
        Cache c = Cache();
        caches.push_back(c);
    }

    // videos
    for (int i = 0; i < V; i++) {
        Video v = Video();
        io >> v.size;
        videos.push_back(v);

        min_video_size = min(min_video_size, v.size);
        total_video_size = total_video_size + v.size;
    }

    // endpoints
    for (int i = 0; i < E; i++) {
        Endpoint e = Endpoint();
        int K;
        io >> e.latency;
        io >> K;
        for (int j = 0; j < K; j++) {
            EndpointCacheConnection ecc = EndpointCacheConnection();
            io >> ecc.cache_id >> ecc.latency;
            ecc.endpoint_id = i;

            e.connections.push_back(ecc);
            caches[ecc.cache_id].connections.push_back(ecc);
        }
        endpoints.push_back(e);
    }

    // requests
    for (int i = 0; i < R; i++) {
        Request r = Request();
        io >> r.video_id >> r.endpoint_id >> r.number_of_requests;
        requests.push_back(r);

        for (int j = 0; j < endpoints[r.endpoint_id].connections.size(); j++) {
            int cache_id = endpoints[r.endpoint_id].connections[j].cache_id;
            int lat = endpoints[r.endpoint_id].connections[j].latency;

            pair<int, int> e = make_pair(r.endpoint_id, lat);

            caches[cache_id].potential_videos[r.video_id].push_back(e);
        }
    }
}

vector<VideoCachePair> parse_reference(fstream &io) {
    int n;
    io >> n;

    vector<VideoCachePair> vcp;

    string dummy;
    getline(io, dummy);

    for (int i = 0; i < n; i++) {
        string line;
        getline(io, line);

        if (line.size() == 0) continue;

        istringstream line_stream(line);

        int cache_id;
        line_stream >> cache_id;

        int video_id;
        while (line_stream >> video_id) {
            VideoCachePair p;
            p.c = cache_id;
            p.v = video_id;
            vcp.push_back(p);
        }
    }

    return vcp;
}

int main(int args, char* argv[]) {
    if (args <= 1) {
        cout << "No input file" << endl;
        exit(1);
    }

    string input_file_path = argv[1];
    fstream input_stream(input_file_path);
    if (! input_stream.good()) {
        cout << "Error parsing input file" << endl;
        exit(1);
    }
    parse_data(input_stream);
    cout << "Done parsing" << endl;

    int longest_reference_solution = 0;
    vector<vector<VideoCachePair>> reference_solutions;
    for (int i = 2; i < args; i++) {
        string reference_file_path = argv[i];
        fstream input_stream(reference_file_path);
        if (! input_stream.good()) {
            cout << "Error parsing input file" << endl;
            exit(1);
        }
        vector<VideoCachePair> ref = parse_reference(input_stream);
        reference_solutions.push_back(ref);
        longest_reference_solution = max(longest_reference_solution, (int) ref.size());
        cout << "Done parsing" << endl;
    }

    // number of genes considering filling all servers with the smallest possible video
    // should probably be much lower
    int number_of_genes = C*X/(min_video_size);
    cout << "Upper bound for # genes " << number_of_genes << endl;

    if (longest_reference_solution != 0) {
        number_of_genes = longest_reference_solution*1.5;
        cout << "Using " << number_of_genes << " genes based on the longest reference solution" << endl;
    }

    cout << "total cache " << total_cache_size << endl;
    cout << "total video " << total_video_size << endl;

    int number_of_chromosomes = 40;
    GA<VideoCachePair> ga = GA<VideoCachePair>(reference_solutions, number_of_chromosomes, number_of_genes);
    return 0;
}
