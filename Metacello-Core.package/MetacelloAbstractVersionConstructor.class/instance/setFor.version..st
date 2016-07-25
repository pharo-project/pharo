api callbacks
setFor: attributeList version: aString
    "conditional symbolicVersion support"

    attributeList asMetacelloAttributeList
        do: [ :attribute | 
            self attributeMap at: attribute put: aString.
            self addAttribute: attribute ]