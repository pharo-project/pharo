accessing
attribute: anObject
    self deprecated: 'Use attributes: instead'.
    self attributes size > 1
        ifTrue: [ self error: 'invalid use of attribute:' ].
    attributes := OrderedCollection with: anObject