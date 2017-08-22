#ifndef MPC_H
#define MPC_H

#include <vector>
#include "Eigen-3.3/Eigen/Core"

using namespace std;

const size_t N = 10;
const double dt = 0.05;
const int latency = 2;

struct Solution {
  vector<double> x;
  vector<double> y;
  vector<double> delta;
  vector<double> a;
};

class MPC {
 public:
  MPC();

  virtual ~MPC();

  // Solve the model given an initial state and polynomial coefficients.
  // Return the first actuatotions.
  //vector<double> Solve(Eigen::VectorXd state, Eigen::VectorXd coeffs);
  Solution Solve(Eigen::VectorXd state, Eigen::VectorXd coeffs);

  double prev_delta {0};
  double prev_a {0.1};

};

#endif /* MPC_H */
