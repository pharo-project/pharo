This class defines magic numbers taken from RFC1951 [1b], and also 
possibly RFC1950 [1a] & RFC1952 [1c]. 
It presets some dictionary structures as described at [1a] 8.1. 

A class can import these bindings as 'class variables' by including the 
following in its class definition: 
   poolDictionaries: 'ZipFileConstants' 

Following are source references to these constants contained within 
several class side initialization methods. 
(These might be better spread through out those methods as time permits) 

initializeDeflateConstants 
    WindowSize [2e]"w_size", [2a]"windowBits is the base two logarithm 
of windowSize where default windowBits=15" 
    WindowMask [2e]"w_mask" 
    MaxDistance [2e]"MAX_DIST" 
    MinMatch [2c] 
    MaxMatch [2c] 
    HashBits [2e]"hash_bits" 
    HashMask [2e]"hash_mask" 
    HashShift [2e]"hash_shift" 

initializeDistanceCodes 
    BaseDistance [2f] 
    DistanceCodes [2f] 

initializeExtraBits 
    ExtraLengthBits [1]3.2.5, [2b] 
    ExtraDistanceBits [1]3.2.5, [2b] 
    ExtraBitLengthBits [2b] 
    BitLengthOrder [1]3.2.7, [2b] 

initializeFixedTrees 
    [1]3.2.6 

initializeWriteStreamConstants 
    MaxBits [2d] 
    MaxBitLengthBits [2b] 
    EndBlock [2b] 
    StoredBlock [2c] 
    FixedBlock [2c] 
    DynamicBlock [2c] [1]3.2.3"BTYPE" 
    NumLiterals 
    MaxLengthCodes [2e] 
    MaxDistCodes [2e] 
    MaxBitLengthCodes [2e] 
    MaxLiteralCodes 
    Repeat3To6 [2b] 
    Repeat3To10 [2b] 
    Repeat11To138 [2b] 

[1a]  "ZLIB Compressed Data Format Specification version 3.3" 
http://www.ietf.org/rfc/rfc1950.txt
[1b] "DEFLATE Compressed Data Format Specification version 1.3" 
http://www.ietf.org/rfc/rfc1951.txt
[1c] "GZIP file format specification version 4.3" 
http://www.ietf.org/rfc/rfc1952.txt
[2] ZLIB Source Code http://zlib.net/zlib-1.2.5.tar.gz
[2a] zlib.h 
[2b] trees.c 
[2c] zutil.h 
[2d] inftrees.c 
[2e] deflate.h 
[2f] trees.h 