printing
printOn: aStream
    aStream
        nextPutAll: self class name;
        nextPut: $(; print: width;
        nextPut: $x; print: height;
        nextPut: $x; print: depth;
        nextPut: $).
