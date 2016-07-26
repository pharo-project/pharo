private
analyzeSpaceSaving

      | elems tablesTotal nonNilTables lastPage lastChunkSize |
      elems := 0.
       tablesTotal := self basicSize.
       nonNilTables := 0.
       lastPage := self basicAt: self basicSize.
       (lastChunkSize := size \\ chunkSize) = 0 ifTrue:
               [lastChunkSize := chunkSize].
       1 to: self basicSize do:
               [:i | | page |
               (page := self basicAt: i) ifNotNil:
                       [nonNilTables := nonNilTables + 1.
                        1 to: (page == lastPage ifTrue: [lastChunkSize] ifFalse: [chunkSize]) do:
                               [:j|
                               (page at: j) ~= defaultValue ifTrue:
                                       [elems := elems + 1]]]].

       ^String streamContents:
               [:strm |
               strm nextPutAll: 'total: '; print: size.
               strm nextPutAll: ' elements: '; print: elems.
               strm nextPutAll: ' tables: '; print: tablesTotal.
               strm nextPutAll: ' non-nil: '; print: nonNilTables]