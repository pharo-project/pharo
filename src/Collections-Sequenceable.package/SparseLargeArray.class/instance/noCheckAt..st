accessing
noCheckAt: index
      ^(self basicAt: index - base // chunkSize + 1)
               ifNil: [defaultValue]
               ifNotNil: [:chunk| chunk at: index - base \\ chunkSize + 1]