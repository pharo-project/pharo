Act as an array of (weak) dependents of some object.

When dependents are reclaimed, they are replaced by an UndefinedObject in the DependentsArray.
This is why instances of this class will take care to iterate only on non nil elements.
These nil also cause loops written as (1 to: self size do: [:i | (self at: i) doSomething]) to be inefficient.
This is because #size and #at: both require scanning for nils.
For this reason, DependentsArray though sequenceable, is not a subclass of SequenceableCollection.