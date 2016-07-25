A variant on the TreeInspector that works "backwards".

By default it shows both weak and strong references (#openOn:) but I can be configured to omit weak refs with #openStrongOn:

Like the TreeInspector, it shows a tree of objects, but expanding a node won't show the objects which that node references, but rather the objects that reference that node.  Its main use is to track down memory leaks: if you want to know why a particular object is still alive, open a PointerExplorer on it and drill down until you find the root object that's referencing it.  For example, find all the references to the symbol #zot with:

EyePointerExplorer openOn: #zot

For the "name" of the object, the PointerExplorer shows each object's identityHash, to allow the user to identify when two similar objects are identical and notice cycles.

