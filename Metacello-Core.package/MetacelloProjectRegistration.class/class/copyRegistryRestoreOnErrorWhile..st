mutability
copyRegistryRestoreOnErrorWhile: aBlock
  "install copy of registry for duration of <aBlock> execution."

  "registrations will be copied on write during <aBlock> execution."

  "if <aBlock> does not return control to this context, revert to the original
	version of the registry. Otherwise leave the new copy installed."

  | oldRegistry newRegistry |
  oldRegistry := self registry.
  newRegistry := self registry copy.
  self registry: newRegistry.
  aBlock
    ensure: [ 
      "install old version of registry"
      newRegistry := self registry.	"see https://github.com/dalehenrich/metacello-work/issues/210"
      self registry: oldRegistry ].
  self registry: newRegistry	"if control returned, install newRegistry"