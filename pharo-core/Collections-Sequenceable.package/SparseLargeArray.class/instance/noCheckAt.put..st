accessing
noCheckAt: index put: value
       | chunkIndex chunk lastChunkSize |
       chunkIndex := index - base // chunkSize + 1.
       (chunk := self basicAt: chunkIndex) ifNil:
               [value = defaultValue ifTrue:
                       [^value].
               chunk := arrayClass
                                       new: ((chunkIndex == self basicSize
                                                  and: [(lastChunkSize := size \\ chunkSize) > 0])
                                                       ifTrue: [lastChunkSize]
                                                       ifFalse: [chunkSize])
                                       withAll: defaultValue.
               self basicAt: chunkIndex put: chunk].
       ^chunk at: index - base \\ chunkSize + 1 put: value