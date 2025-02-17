#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Partition function from a standard C++ text
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Choose the last element as pivot
    int i = low - 1; // Pointer for the smaller element

    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(arr[i], arr[j]);
        }
    }
    swap(arr[i + 1], arr[high]);
    return i + 1; // Return partition index
}

// Iterative Quicksort function
void quicksort(vector<int>& arr) {
    stack<pair<int, int>> stk;
    stk.push({0, arr.size() - 1});

    while (!stk.empty()) {
        int low = stk.top().first;
        int high = stk.top().second;
        stk.pop();

        if (low < high) {
            int pivot = partition(arr, low, high);

            // Push left and right subarrays to stack
            if (pivot - 1 > low) {
                stk.push({low, pivot - 1});
            }
            if (pivot + 1 < high) {
                stk.push({pivot + 1, high});
            }
        }
    }
}

// Utility function to print an array
void printArray(const vector<int>& arr) {
    for (int num : arr) {
        cout << num << " ";
    }
    cout << endl;
}

int main() {
    vector<int> arr = {1, 5, 9, 12, 7, 7};
    cout << "Original array: ";
    printArray(arr);

    quicksort(arr);

    cout << "Sorted array: ";
    printArray(arr);
    return 0;
}
