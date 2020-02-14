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

struct pair_comparator {
	template<typename T>
	bool operator()(const T& l, const T& r) const {
		return l.first < r.first;
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
        vector<EndpointCacheConnection> connections;

        // video id -> list of endpoints
        // used to avoid looping over all connected cache servers
        unordered_map<int, vector<pair<int, int>>> potential_videos;
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

class Chromosome {
    public:
        vector<VideoCachePair> genes;
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

            vector<int> space_left(caches.size(), X);

            // endpoint, video
            unordered_map<pair<int, int>, int, hash_pair> lowest_latency;

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

        Chromosome onepoint_cross(Chromosome &c) {
            vector<VideoCachePair> new_genes(genes.size());

            int crossover_point = rand() % genes.size();

            for (int i = 0; i < genes.size(); i++) {
                if (i > crossover_point) {
                    new_genes[i] = genes[i];
                } else {
                    new_genes[i] = c.genes[i];
                }
            }

            Chromosome n = Chromosome(new_genes);
            return n;
        }

        Chromosome twopoint_cross(Chromosome &c) {
            vector<VideoCachePair> new_genes(genes.size());

            int tmp1 = rand() % genes.size();
            int tmp2 = rand() % genes.size();

            int crossover_point_1 = min(tmp1, tmp2);
            int crossover_point_2 = max(tmp1, tmp2);

            for (int i = 0; i < genes.size(); i++) {
                if (i > crossover_point_1 && i < crossover_point_2) {
                    new_genes[i] = genes[i];
                } else {
                    new_genes[i] = c.genes[i];
                }
            }

            Chromosome n = Chromosome(new_genes);
            return n;
        }

        Chromosome uniform_cross(Chromosome &c) {
            vector<VideoCachePair> vcp(genes.size());

            for (int i = 0; i < genes.size(); i++) {
                if (rand() % 2 == 0) {
                    vcp[i] = genes[i];
                } else {
                    vcp[i] = c.genes[i];
                }
            }

            return Chromosome(vcp);
        }
};

class GA {
    public:
        vector<Chromosome> chromosomes;

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

        int weighted_random_index() {
            int count = chromosomes.size();

            for (int i = 0; i < count; i++) {
                if (rand() % 2 == 0) {
                    return i;
                }
            }
            return count-1;
        }

        vector<pair<int, int>> sorted_score_cache;
        int sorted_score_cache_id = -1;
        int rank_selection() {
            if (sorted_score_cache_id != generation) {
                sorted_score_cache = vector<pair<int, int>>(chromosomes.size());

                for (int i = 0; i < chromosomes.size(); i++) {
                    sorted_score_cache[i] = make_pair(-chromosomes[i].evaluate(), i);
                }

                sort(sorted_score_cache.begin(), sorted_score_cache.end(), pair_comparator());
                sorted_score_cache_id = generation;
            }

            int index = weighted_random_index();
            return sorted_score_cache[index].second;
        }

        void cross_chromosomes() {
            vector<Chromosome> new_chromosomes;
            new_chromosomes.reserve(chromosomes.size());

            for (int i = 0; i < chromosomes.size(); i++) {
                int i1 = rank_selection();
                int i2 = rank_selection();

                int type = rand() % 4;
                // 1/4 uniform
                // 1/4 twopoint
                // 2/4 onepoint
                if (type == 0) {
                    Chromosome new_chromosome = chromosomes[i1].uniform_cross(chromosomes[i2]);
                    new_chromosomes.push_back(new_chromosome);
                } else if (type == 1) {
                    Chromosome new_chromosome = chromosomes[i1].twopoint_cross(chromosomes[i2]);
                    new_chromosomes.push_back(new_chromosome);
                } else {
                    Chromosome new_chromosome = chromosomes[i1].onepoint_cross(chromosomes[i2]);
                    new_chromosomes.push_back(new_chromosome);
                }
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
