A set of characters.  Lookups for inclusion are very fast.

map is a ByteArray of size 256, initially all zeros.  
map at: n+1 is set to 1 when a Character with a codePoint of n is added to the set.

If a "wide" character (codePoint > 255) is added, the ChacracterSet becomes a WideCharacterSet.
 