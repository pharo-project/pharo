api callbacks
setFor: attributeList version: aString
    "conditional symbolicVersion support"

    self methodSections
        add:
            (MetacelloSymbolicVersionSpec new
                attributes: attributeList asMetacelloAttributeList;
                versionString: aString;
                yourself)