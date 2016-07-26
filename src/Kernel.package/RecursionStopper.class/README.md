The RecursionStopper provides an easy way to check if we are in a recursion and execute code just once in a recursion.

RecursionStopper during:  aBlock.

executes a block just once in a recursion.

A RecursionStopper object contains a collection of active methods which are currently called from within RecrusionStopper>>#during: this means that Recursion stopper can be used multiple places without one blocking the other, but multiple stoppers cannot be nested in the same method.