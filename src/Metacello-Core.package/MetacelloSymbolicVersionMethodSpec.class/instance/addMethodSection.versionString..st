adding
addMethodSection: attribute versionString: aString
    self methodSections
        add:
            (MetacelloSymbolicVersionSpec new
                attributes: attribute asMetacelloAttributeList;
                versionString: aString;
                yourself)