operating
forArrays: anArray processWith: aBlock

 "  anArray is a kind of a sequenceable collection of arrays.
    aBlock is a block with one argument, that is used to process a  tuple immediately after it is constructed. "
  collectionOfArrays := anArray.
  resultProcessingBlock := aBlock.
  buffer := Array new: anArray size.
  self combineFromIdx: 1
   