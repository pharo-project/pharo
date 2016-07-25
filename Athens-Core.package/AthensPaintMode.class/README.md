A paint mode controls how the incoming color (source) will be transferred to surface (destination).

To set a paint mode for canvas, send a message with corresponding mode name to my instance.
My instance(s) is available via canvas protocol, i.e. 'canvas paintMode'.

For example, to set an 'over' paint mode, use following:

	canvas paintMode over.

If you want to restore original paint mode after performing drawing operations, which may change it, use #restoreAfter: method , i.e. 

	canvas paintMode restoreAfter: [
		.. perform any drawing operations here..
		].

Since different backends may support different set of paint modes, the default implementation in AthensPaintMode for 
all mode setters is to signal an error.
To query a set of available paint modes, supported by backend, use #availableModes message.

