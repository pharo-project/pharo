api callbacks
setFor: attributeList do: aBlock
    "conditional version support"

    | methodSection |
    methodSection := MetacelloVersionMethodSection new
        attributes: attributeList asMetacelloAttributeList;
        block: aBlock;
        yourself.
    currentSection ~~ nil
        ifTrue: [ currentSection addMethodSection: methodSection ]
        ifFalse: [ self methodSections add: methodSection ]