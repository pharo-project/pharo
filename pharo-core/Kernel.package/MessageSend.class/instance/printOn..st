printing
printOn: aStream

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        receiver printOn: aStream.
        aStream nextPut: $)