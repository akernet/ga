#include <iostream>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include <bits/stdc++.h>
#include <stdlib.h>

using namespace std;

struct hash_pair {
    template <class T1, class T2>
    size_t operator()(const pair<T1, T2>& p) const
    {
        auto hash1 = hash<T1>{}(p.first);
        auto hash2 = hash<T2>{}(p.second);
        return hash1 ^ hash2;
    }
};

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
        vector<EndpointCacheConnection> connections = vector<EndpointCacheConnection>();

        // video id -> list of endpoints
        // used to avoid looping over all connected cache servers
        unordered_map<int, vector<pair<int, int>>> potential_videos = unordered_map<int, vector<pair<int, int>>>();
};

class Endpoint {
    public:
        int latency;
        vector<EndpointCacheConnection> connections = vector<EndpointCacheConnection>();
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

vector<Video> videos = vector<Video>();
vector<Cache> caches = vector<Cache>();
vector<Endpoint> endpoints = vector<Endpoint>();
vector<Request> requests = vector<Request>();
int V, E, R, C, X;

class Chromosome {
    public:
        vector<VideoCachePair> genes = vector<VideoCachePair>();
        int score = -1;

        Chromosome(int number_of_genes) {
            for (int i = 0; i < number_of_genes; i++) {
                VideoCachePair vcp = VideoCachePair();

                // Generate random video-cache-pair
                vcp.c = rand() % (int) caches.size();
                vcp.v = rand() % (int) videos.size();

                genes.push_back(vcp);
            }
        }

        Chromosome(vector<VideoCachePair> c) {
            genes = c;
        }

        Chromosome(const Chromosome &ref) {
            genes = vector<VideoCachePair>(ref.genes);
            score = ref.score;
        }

        int evaluate() {
            if (score != -1) {
                return score;
            }

            vector<int> space_left = vector<int>(caches.size(), X);

            // endpoint, video
            unordered_map<pair<int, int>, int, hash_pair> lowest_latency = unordered_map<pair<int, int>, int, hash_pair>();

            for (int i = 0; i < genes.size(); i++) {
                VideoCachePair vc = genes[i];
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
            for (int i = 0; i < requests.size(); i++) {
                Request r = requests[i];
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

        void mutate() {

            for (int i = 0; i < genes.size(); i++) {

                if (rand() % genes.size() == 0) {
                    // randomly change cache or video
                    if (rand() % 2 == 0) {
                        genes[i].c = rand() % (int) caches.size();
                    } else {
                        genes[i].v = rand() % (int) videos.size();
                    }

                    // invalidate score
                    score = -1;
                }
            }

        }

        Chromosome cross_copy(Chromosome &c) {

            vector<VideoCachePair> a1 = vector<VideoCachePair>();
            a1.reserve(genes.size());

            int crossover_point = rand() % genes.size();

            for (int i = 0; i < genes.size(); i++) {
                if (i > crossover_point) {
                    a1.push_back(genes[i]);
                } else {
                    a1.push_back(c.genes[i]);
                }
            }

            Chromosome n = Chromosome(a1);
            return n;
        }
};

class GA {
    public:
        vector<Chromosome> chromosomes = vector<Chromosome>();

        int best_score = 0;
        int generation = 0;

        GA(int number_of_chromosomes, int number_of_genes) {
            for (int i = 0; i < number_of_chromosomes; i++) {
                Chromosome c = Chromosome(number_of_genes);
                chromosomes.push_back(c);
            }

            while (true) {

                evaluate_chromosomes();
                if (generation % 10 == 0) {
                    cout << "Generation: ";
                    cout << setw(10) << setfill('0') << generation;
                    cout << " best score: ";
                    cout << setw(10) << setfill('0') << get_best_chromosome().evaluate();
                    cout << endl;
                    //print_population_fitness();
                }

                // elitism
                Chromosome best = get_best_chromosome();

                cross_chromosomes();
                mutate_chromosomes();

                // elitism
                chromosomes[0] = Chromosome(best);

                generation += 1;
            }

        }

        void evaluate_chromosomes() {
            #pragma omp parallel for schedule(dynamic)
            for (int i = 0; i < chromosomes.size(); i++) {
                int time_saved = chromosomes[i].evaluate();
                if (time_saved > best_score) {
                    best_score = time_saved;
                }
            }
        }

        void mutate_chromosomes() {
            #pragma omp parallel for schedule(dynamic)
            for (int i = 0; i < chromosomes.size(); i++) {
                chromosomes[i].mutate();
            }
        }

        // could be made much faster
        int weighted_random_index(vector<Chromosome> &c) {
            int sum_of_weight = 0;
            for(int i=0; i < c.size(); i++) {
                sum_of_weight += c[i].evaluate();
            }
            int rnd = rand() % sum_of_weight;
            for(int i = 0; i < c.size(); i++) {
                if(rnd < c[i].evaluate()) {
                    return i;
                }
                rnd -= c[i].evaluate();
            }
            assert(false);
        }

        void cross_chromosomes() {
            vector<Chromosome> new_chromosomes = vector<Chromosome>();
            new_chromosomes.reserve(chromosomes.size());

            for (int i = 0; i < chromosomes.size(); i++) {
                int i1 = weighted_random_index(chromosomes);
                int i2 = weighted_random_index(chromosomes);

                Chromosome new_chromosome = chromosomes[i1].cross_copy(chromosomes[i2]);

                new_chromosomes.push_back(new_chromosome);
            }

            chromosomes = new_chromosomes;
        }

        void print_population_fitness() {
            for (int i = 0; i < chromosomes.size(); i++) {
                cout << chromosomes[i].evaluate() << " ";
            }
            cout << endl;
        }

        Chromosome get_best_chromosome() {
            int best_fitness = 0;
            int best_index = 0;
            for (int i = 0; i < chromosomes.size(); i++) {
                if (chromosomes[i].evaluate() > best_fitness) {
                    best_fitness = chromosomes[i].evaluate();
                    best_index = i;
                }
            }
            Chromosome best_chromosome = chromosomes[best_index];
            return best_chromosome;
        }
};

int min_video_size = INT_MAX;

long long total_video_size = 0;
long long total_cache_size = 0;

void parse_data() {
    cin >> V >> E >> R >> C >> X;

    total_cache_size = C*X;

    // cache servers
    for (int i = 0; i < C; i++) {
        Cache c = Cache();
        caches.push_back(c);
    }

    // videos
    for (int i = 0; i < V; i++) {
        Video v = Video();
        cin >> v.size;
        videos.push_back(v);

        min_video_size = min(min_video_size, v.size);
        total_video_size = total_video_size + v.size;
    }

    // endpoints
    for (int i = 0; i < E; i++) {
        Endpoint e = Endpoint();
        int K;
        cin >> e.latency;
        cin >> K;
        for (int j = 0; j < K; j++) {
            EndpointCacheConnection ecc = EndpointCacheConnection();
            cin >> ecc.cache_id >> ecc.latency;
            ecc.endpoint_id = i;

            e.connections.push_back(ecc);
            caches[ecc.cache_id].connections.push_back(ecc);
        }
        endpoints.push_back(e);
    }

    // requests
    for (int i = 0; i < R; i++) {
        Request r = Request();
        cin >> r.video_id >> r.endpoint_id >> r.number_of_requests;
        requests.push_back(r);

        for (int j = 0; j < endpoints[r.endpoint_id].connections.size(); j++) {
            int cache_id = endpoints[r.endpoint_id].connections[j].cache_id;
            int lat = endpoints[r.endpoint_id].connections[j].latency;

            pair<int, int> e = make_pair(r.endpoint_id, lat);

            caches[cache_id].potential_videos[r.video_id].push_back(e);
        }
    }
}

int main() {
    parse_data();
    cout << "Done parsing" << endl;
    cout << "Min size " << min_video_size << endl;

    // number of genes considering filling all servers with the smallest possible video
    // should probably be much lower
    int number_of_genes = C*X/(min_video_size);
    cout << "upper bound for # genes " << number_of_genes << endl;

    cout << "total cache " << total_cache_size << endl;
    cout << "total video " << total_video_size << endl;

    int number_of_chromosomes = 100;
    GA ga = GA(number_of_chromosomes, number_of_genes);
    return 0;
}
