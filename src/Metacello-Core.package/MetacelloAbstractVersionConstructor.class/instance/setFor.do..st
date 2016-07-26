api callbacks
setFor: attributeList do: aBlock
    "conditional version support"

    attributeList asMetacelloAttributeList
        do: [ :attribute | 
            | blockList |
            blockList := self attributeMap
                at: attribute
                ifAbsent: [ self attributeMap at: attribute put: (blockList := OrderedCollection new) ].
            blockList add: aBlock.
            self addAttribute: attribute ]