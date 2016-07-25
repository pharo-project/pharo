mutability
copyRegistryWhile: aBlock
    "install copy of registry for duration of <aBlock> execution."

    "registrations will be copied on write during <aBlock> execution."

    "Unconditionally revert to the original
	version of the registry. Otherwise leave the new copy installed."

    | oldRegistry newRegistry |
    oldRegistry := self registry.
    newRegistry := self registry copy.
    self registry: newRegistry.
    aBlock
        ensure: [ 
            "install old version of registry"
            self registry: oldRegistry ]