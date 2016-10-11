Halt is a special exception provided to support breakpoints as defined in  Object>>halt.

!! Basic usage

To manually set a breakpoint you can use message halt as follows: 
[[[			
anObject halt. 
]]]
You can also use 

[[[
Halt now
]]]

[[[
Halt now: 'With a nice explanation'
]]]

!! Advanced usage
The class Halt supports different halting mechanisms such as conditionals, based on iteration or simply stopping the first time. 

!!! Conditionals

[[[
foo
	...
	Halt if: #invokedFromThisSelector
]]]

This expression will only stop the execution  of method foo if this method is in the call chain (is invoked from ) message invokedFromThisSelector.

[[[
foo
      ...
      Halt if: aBlock		
]]]

The execution of foo will stop if aBlock returns true. 

!!! Counting
Sometimes we need to stop after a given number of iteration.

[[[
Halt count: 5
]]]



!!! Once
It is really useful sometimes to stop only and only one time

[[[
foo
	...
	Halt once. 
]]]

It will be stopped the first time. 
Then if you need to rearm it to stop another time you should use

[[[
Halt resetOnce			
]]]


