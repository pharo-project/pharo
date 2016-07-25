I am a special layout for weak variable sized objects such as WeakArray.

I contain a fixed number of Slots plus.
Instances of classes using this kind of layout have only a minimum given size. Instances have a custom number of additional weak fields which can be accessed with an index.

References held in the variable part are held weakly and might be nilled out by the garbage collector at any time. References in the named section are held strongly as in the default layout.