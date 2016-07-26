*monticellofiletree-core
writeCypressJsonOn: aStream forHtml: forHtml indent: startIndent
    "by default ignore <forHtml> ... <forHtml> is used for Dictionary and Array, i.e., container objects and String which actually encodes itself differently for HTML"

    | indent |
    aStream
        nextPutAll: '[';
        lf.
    indent := startIndent + 1.
    1 to: self size do: [ :index | 
        | item |
        item := self at: index.
        aStream tab: indent.
        item writeCypressJsonOn: aStream forHtml: forHtml indent: indent.
        index < self size
            ifTrue: [ 
                aStream
                    nextPutAll: ',';
                    lf ] ].
    self size = 0
        ifTrue: [ aStream tab: indent ].
    aStream nextPutAll: ' ]'