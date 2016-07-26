accessing
packages
    "leave reference to packages for upgrade purposes"

    packages == nil
        ifTrue: [ ^ super packages ].
    ^ packages