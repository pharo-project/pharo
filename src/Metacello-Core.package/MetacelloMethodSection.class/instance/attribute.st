accessing
attribute
    self deprecated: 'Use attributes instead'.
    self attributes size > 1
        ifTrue: [ self error: 'invalid use of attribute' ].
    self attributes isEmpty
        ifTrue: [ ^ nil ].
    ^ self attributes first