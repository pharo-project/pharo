This is an example for a virtual slot that computes its value. 

It stores a block which is evaluated with the object as a parameter to calculate the value
on read. Writing is ignored.

e.g. make a class lile this:

Object subclass: #TT
	slots: { #i => ComputedSlot with: [ :o | o class methods size ] }
	classVariables: {  }
	category: 'TT'