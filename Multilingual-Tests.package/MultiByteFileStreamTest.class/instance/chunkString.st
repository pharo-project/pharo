support
chunkString
  "A chunk larger than buffer (currently 2048), but not long enough to fill it completely again. Therefore, buffer should contain some elements at end that are not part of the real buffer"
  | smallChunk chunkString |
  chunkString := 
  String new: (MultiByteFileStream new ascii enableReadBuffering braceArray size * 1.3) floor.
  smallChunk := 'ASDFASDFASDFASDFasdfasdfasdfasdfQWERQWERQWERqwerqwerqwer! !'.
  1 to: chunkString size by: smallChunk size do: [:ix |
    chunkString replaceFrom: ix to: (ix + smallChunk size -1 min: chunkString size) with: smallChunk startingAt: 1].
  ^chunkString