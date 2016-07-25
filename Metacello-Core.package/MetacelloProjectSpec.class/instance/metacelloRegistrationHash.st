scripting
metacelloRegistrationHash
    "name className versionString operator loads preLoadDoIt postLoadDoIt"

    | hash |
    hash := String stringHash: name initialHash: 0.
    hash := String stringHash: self className initialHash: hash.
    hash := String stringHash: self versionString initialHash: hash.
    hash := String stringHash: self operator asString initialHash: hash.
    hash := String stringHash: self preLoadDoIt asString initialHash: hash.
    hash := String stringHash: self postLoadDoIt asString initialHash: hash.
    ^ hash bitXor: loads hash