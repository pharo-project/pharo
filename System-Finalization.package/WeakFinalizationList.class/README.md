IMPORTANT!!!

This class is a special object, recognized by VM.
Its only purpose is to 
a) identify a special kind of objects who usually having a weak references but
  also having an instance of me held by first non-weak fixed slot (instance variable).

b) a 'first' instance variable points to the head of a list of items, reported by VM which has weak references which became garbage during last garbage collection

At my class side, there are some public behavior, which is used by finalization process to detect if VM supports new finalization scheme or should use the old one.
Weak registry using #hasNewFinalization for switching to correct finalization logic,
depending on VM it currently runs on.
