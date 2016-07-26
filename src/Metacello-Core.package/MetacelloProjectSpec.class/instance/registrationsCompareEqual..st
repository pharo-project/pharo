scripting
registrationsCompareEqual: aMetacelloProjectSpec
    "name className versionString operator loads preLoadDoIt postLoadDoIt"

    ^ self className = aMetacelloProjectSpec className
        and: [ self versionString = aMetacelloProjectSpec versionString and: [ self operator == aMetacelloProjectSpec operator ] ]