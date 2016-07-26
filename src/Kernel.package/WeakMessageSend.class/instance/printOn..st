printing
printOn: aStream

        aStream
                nextPutAll: self class name;
                nextPut: $(.
        selector printOn: aStream.
        aStream nextPutAll: ' -> '.
        self receiver printOn: aStream.
        aStream nextPut: $)
