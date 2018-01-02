#include <numeric>
#include <map>
#include <vector>
#include <z3++.h>

using namespace std;
using namespace z3;

int main()
{
    context c;
    solver s (c);
    const size_t sz = 9;
    vector<vector<expr>> grid;
    vector<vector<int>> hints (9, vector<int> (9));

    // Hints.
    for (size_t i = 0; i < sz; ++i) {
        for (size_t j = 0; j < sz; ++j) {
            int d;
            if (!(cin >> d)) {
                cerr << "Invalid input" << endl;
                exit(1);
            }
            hints[i][j] = d;
        }
    }

    // Init grid.
    for (size_t i = 0; i < sz; ++i) {
        vector<expr> buf;
        for (size_t j = 0; j < sz; ++j) {
            ostringstream oss;
            oss << i << "," << j;
            auto str = oss.str().c_str();
            buf.push_back(c.int_const(str));

            // cout << str << endl;
        }
        grid.push_back(buf);
    }

    // Constraints.
    // Individual elements in range, horizontal and vertical rows are distinct.
    for (size_t i = 0; i < sz; ++i) {
        expr_vector buf1 (c);
        expr_vector buf2 (c);

        for (size_t j = 0; j < sz; ++j) {
            s.add(grid[i][j] >= 1 && grid[i][j] <= 9);

            buf1.push_back(grid[i][j]);
            buf2.push_back(grid[j][i]);

            // dbg1.push_back(make_pair(i,j));
            // dbg2.push_back(make_pair(j,i));
        }

        s.add(distinct(buf1));
        s.add(distinct(buf2));
    }

    // Distinct within a field.
    for (size_t i = 0; i < sz - 2; i+= 3) {
        for (size_t j = 0; j < sz - 2; j+= 3) {
            expr_vector buf (c);
            buf.push_back(grid[0 + j][0 + i]);
            buf.push_back(grid[0 + j][1 + i]);
            buf.push_back(grid[0 + j][2 + i]);

            buf.push_back(grid[1 + j][0 + i]);
            buf.push_back(grid[1 + j][1 + i]);
            buf.push_back(grid[1 + j][2 + i]);

            buf.push_back(grid[2 + j][0 + i]);
            buf.push_back(grid[2 + j][1 + i]);
            buf.push_back(grid[2 + j][2 + i]);

            s.add(distinct(buf));
            // cout << "x:" << j << ", y: " << i << endl;
            // cout << "size: " << buf.size() << endl;
        }
    }

    auto add_hint = [&grid, &c, &s](int i, int j, int val) {
        expr_vector buf (c);
        
        if (j - 1 >= 0 &&
            i - 1 >= 0) {
            buf.push_back(grid[i][j - 1] * grid[i - 1][j] == val);
        }

        if (j - 1 >= 0 &&
            i + 1 <= 8) {
            buf.push_back(grid[i][j - 1] * grid[i + 1][j] == val);
        }

        if (i + 1 <= 8 &&
            j + 1 <= 8) {
            buf.push_back(grid[i + 1][j] * grid[i][j + 1] == val);
        }

        if (j + 1 <= 8 &&
            i - 1 >= 0) {
            buf.push_back(grid[i][j + 1] * grid[i - 1][j] == val);
        }

        if (i - 1 >= 0 &&
            i + 1 <= 8) {
            buf.push_back(grid[i - 1][j] * grid[i + 1][j] == val);
        }

        if (j - 1 >= 0 &&
            j + 1 <= 8) {
            buf.push_back(grid[i][j - 1] * grid[i][j + 1] == val);
        }

        s.add(mk_or(buf));
    };

    // Hints.
    for (size_t i = 0; i < sz; ++i) {
        for (size_t j = 0; j < sz; ++j) {
            if (hints[i][j] != 0) {
                cout << "Adding hint: " << hints[i][j]
                     << " at (" << i << "," << j << ")" << endl;
                add_hint(i, j, hints[i][j]);
            }
        }
    }

    // Solve.
    cout << "model: " << endl << s << endl;
    cout << "check: " << s.check() << endl;

    model m = s.get_model();
    for (size_t i = 0; i < sz; ++i) {
        for (size_t j = 0; j < sz; ++j) {
            cout << "(" << i << "," << j << ": " << m.eval(grid[i][j]) << ")";
            if (j > 0 && (j + 1) % 3 == 0) {
                cout << " ";
            }
        }
        cout << endl;
        if (i > 0 && (i + 1) % 3 == 0) {
            cout << endl;
        }
    }

    // Get result.
    int res = 0;
    for (size_t i = 0; i < sz; ++i) {
        for (size_t j = 0; j < sz; ++j) {
            if (hints[i][j] != 0) {
                int x = m.eval(grid[i][j]).get_numeral_int();
                cout << "Value: " << x
                     << " at (" << i << "," << j << ")" << endl;
                int sq = x * x;
                cout << "Square: " << sq << endl;
                res += sq;
                cout << "Result so far: " << res << endl;
            }
        }
        cout << endl;
    }

    cout << "Result: " << res << endl;

    return 0;
}
