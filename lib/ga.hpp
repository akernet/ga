#include <vector>
#include <stdlib.h>

#include "robin_hood.h"

using namespace std;

#define rn() fastrand()
#define umap robin_hood::unordered_flat_map

static unsigned int g_seed = rand();
inline int fastrand() { 
  g_seed = (214013*g_seed+2531011); 
  return (g_seed>>16)&0x7FFF; 
} 


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

template <typename T>
class Chromosome {
    public:
        vector<T> genes;
        int score = -1;

        Chromosome() {}
        Chromosome(int number_of_genes);
        Chromosome(vector<T> c, int number_of_genes);
        int evaluate();
        void mutate();

        void onepoint_cross(Chromosome &c, Chromosome& dst) {
            int crossover_point = rn() & genes.size();

            for (int i = 0; i < genes.size(); i++) {
                if (i >= crossover_point) {
                    dst.genes[i] = genes[i];
                } else {
                    dst.genes[i] = c.genes[i];
                }
            }
        }

        void twopoint_cross(Chromosome &c, Chromosome& dst) {
            int tmp1 = rn() % genes.size();
            int tmp2 = rn() % genes.size();

            int crossover_point_1 = min(tmp1, tmp2);
            int crossover_point_2 = max(tmp1, tmp2);

            for (int i = 0; i < genes.size(); i++) {
                if (i > crossover_point_1 && i < crossover_point_2) {
                    dst.genes[i] = genes[i];
                } else {
                    dst.genes[i] = c.genes[i];
                }
            }
        }

        void uniform_cross(Chromosome &c, Chromosome& dst) {
            for (int i = 0; i < genes.size(); i++) {
                if (rn() % 2 == 0) {
                    dst.genes[i] = genes[i];
                } else {
                    dst.genes[i] = c.genes[i];
                }
            }
        }
};

template <typename T>
class GA {
    public:
        vector<Chromosome<T> > chromosomes_a;
        vector<Chromosome<T> > chromosomes_b;

        bool vector_a = true;

        int generation = 0;

        GA(vector<vector<T>> references, int number_of_chromosomes, int number_of_genes) {
            chromosomes_a = vector<Chromosome<T> >(number_of_chromosomes);
            chromosomes_b = vector<Chromosome<T> >(number_of_chromosomes);

            vector<Chromosome<T> >& chromosomes = chromosomes_a;

            for (int i = 0; i < references.size(); i++) {
                //chromosomes.push_back(Chromosome(references[i], number_of_genes));
                chromosomes_a[i] = Chromosome<T>(number_of_genes);
                cout << "Reference solution " << i << " has score " << chromosomes[i].evaluate() << endl;
            }

            for (int i = references.size(); i < number_of_chromosomes; i++) {
                chromosomes_a[i] = Chromosome<T>(number_of_genes);
            }

            chromosomes_b = chromosomes_a;

            auto start_time = chrono::system_clock::now();

            auto current_time = start_time;
            double duration = 0;
            
            float a = 0;
            float b = 0;

            while (true) {
                auto generation_start_time = chrono::system_clock::now();

                evaluate_chromosomes();

                current_time = chrono::system_clock::now();
                a = 0.5*a + 0.5*chrono::duration<double>(current_time-generation_start_time).count();

                if (generation % 10 == 0 && generation > 0) {
                    current_time = chrono::system_clock::now();
                    duration = chrono::duration<double>(current_time-start_time).count();

                    cout << "Time (s): ";
                    cout << setw(6) << setfill('0') << (int) duration;
                    cout << " generation: ";
                    cout << setw(10) << setfill('0') << generation;
                    cout << " best score: ";
                    cout << setw(10) << setfill('0') << get_best_chromosome().evaluate();
                    cout << " speed (generations/s) ";
                    cout << fixed << setprecision(4) << setw(8) << setfill('0') << ((double) generation/max(0.0, duration));
                    cout << " # c " << chromosomes.size() << " # g " << chromosomes[0].genes.size();
                    cout << endl;

                    cout << "avg eval (s) " << max(0.0, a) << " avg cross/mut " << max(0.0, b-a) << endl;
                }

                // elitism
                Chromosome<T> best = get_best_chromosome();

                cross_chromosomes();
                mutate_chromosomes();

                current_time = chrono::system_clock::now();
                b = 0.5*b + 0.5*chrono::duration<double>(current_time-generation_start_time).count();

                // elitism
                chromosomes_a[0] = best;
                chromosomes_b[0] = best;

                generation += 1;
            }

        }

        void evaluate_chromosomes() {
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            #pragma omp parallel for schedule(guided)
            for (int i = 0; i < chromosomes.size(); i++) {
                chromosomes[i].evaluate();
            }
        }

        void mutate_chromosomes() {
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            for (int i = 0; i < chromosomes.size(); i++) {
                chromosomes[i].mutate();
            }
        }

        // could be made much faster
        int weighted_random_index(vector<Chromosome<T> > &c) {
            int sum_of_weight = 0;
            for(int i=0; i < c.size(); i++) {
                sum_of_weight += c[i].evaluate();
            }
            int rnd = rn() % sum_of_weight;
            for(int i = 0; i < c.size(); i++) {
                if(rnd < c[i].evaluate()) {
                    return i;
                }
                rnd -= c[i].evaluate();
            }
            assert(false);
        }

        int weighted_random_index() {
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            int count = min((int) chromosomes.size(), 10);

            for (int i = 0; i < count; i++) {
                if (rn() % 2 == 0) {
                    return i;
                }
            }
            return count-1;
        }

        vector<pair<int, int>> sorted_score_cache;
        int sorted_score_cache_id = -1;

        int rank_selection() {
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            if (sorted_score_cache_id != generation) {
                if (generation == 0) {
                    sorted_score_cache = vector<pair<int, int>>(chromosomes.size());
                }

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
            // ping pong vectors
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            vector<Chromosome<T> >& new_chromosomes = (!vector_a) ? chromosomes_a : chromosomes_b;

            for (int i = 0; i < chromosomes.size(); i++) {
                int i1 = rank_selection();
                int i2 = rank_selection();

                // int i1 = weighted_random_index(chromosomes);
                // int i2 = weighted_random_index(chromosomes);
                
                int type = rn() % 4;
                // 1/4 uniform
                // 1/4 twopoint
                // 2/4 onepoint
                if (type == 0) {
                    //chromosomes[i1].uniform_cross(chromosomes[i2], new_chromosomes[i]);
                    chromosomes[i1].onepoint_cross(chromosomes[i2], new_chromosomes[i]);
                } else if (type == 1) {
                    //chromosomes[i1].twopoint_cross(chromosomes[i2], new_chromosomes[i]);
                    chromosomes[i1].onepoint_cross(chromosomes[i2], new_chromosomes[i]);
                } else {
                    chromosomes[i1].onepoint_cross(chromosomes[i2], new_chromosomes[i]);
                }
            }

            vector_a = !vector_a;
        }

        Chromosome<T> get_best_chromosome() {
            vector<Chromosome<T> >& chromosomes = vector_a ? chromosomes_a : chromosomes_b;
            int best_fitness = INT_MIN;
            int best_index = 0;
            for (int i = 0; i < chromosomes.size(); i++) {
                if (chromosomes[i].evaluate() > best_fitness) {
                    best_fitness = chromosomes[i].evaluate();
                    best_index = i;
                }
            }
            return chromosomes[best_index];
        }
};