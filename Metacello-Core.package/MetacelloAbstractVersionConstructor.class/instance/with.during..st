private
with: aMetacelloSpec during: aBlock
    | previousRoot |
    previousRoot := self root.
    self root: aMetacelloSpec.
    aBlock
        ensure: [ self root: previousRoot ]