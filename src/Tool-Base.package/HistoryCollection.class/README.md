I'm a data structure used to store an history.

My behavior is
	- I store Associations (anElement -> anInteger), where anElement is the element to store and anInteger the number of times it has been added. (as a Dictionary)
	- I only store once each element (as a Set).
	- I have a max size. If I add an element and have reached the max size, I remove the less seen element.
	- I store element in the chronological order (as an OrderedCollection)