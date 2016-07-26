write and read
nextPutAll: aCollection
    "Write each of the objects in aCollection to the
     receiver stream. Answer aCollection."

    ^ aCollection do: [:each | self nextPut: each]