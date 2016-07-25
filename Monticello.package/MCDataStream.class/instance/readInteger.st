write and read
readInteger
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextInt32	"signed!!!"