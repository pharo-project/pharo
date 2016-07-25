This class adds the following optimizations to the basic Inflate decompression:

a) Bit reversed access
If we want to fetch the bits efficiently then we have them in the wrong bit order (e.g., when we should fetch 2r100 we would get 2r001). But since the huffman tree lookup determines the efficiency of the decompression, reversing the bits before traversal is expensive. Therefore the entries in each table are stored in REVERSE BIT ORDER. This is achieved by a reverse increment of the current table index in the huffman table construction phase (see method increment:bits:). According to my measures this speeds up the implementation by about 30-40%.

b) Inplace storage of code meanings and extra bits
Rather than looking up the meaning for each code during decompression of blocks we store the appropriate values directly in the huffman tables, using a pre-defined mapping. Even though this does not make a big difference in speed, it cleans up the code and allows easier translation into primitive code (which is clearly one goal of this implementation).

c) Precomputed huffman tables for fixed blocks
So we don't have to compute the huffman tables from scratch. The precomputed tables are not in our superclass to avoid double storage (and my superclass is more intended for documentation anyways).