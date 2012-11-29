% Demonstration of cFibHeap class


% Create initially empty heap
heap = cFibHeap

% Insert keys into heap
heap.insert(3)
heap.insert(5)
heap.insert(20)
heap.insert(11)
heap.insert(46)
heap.insert(38)
heap.insert(17)

% View size of heap
heap.n

% Retrieve minimum key
heap.findMin

% Extract minimum key
heap.extractMin

