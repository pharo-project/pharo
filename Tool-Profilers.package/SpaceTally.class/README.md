I'm responsible to help getting information about system space usage. The information I compute is represented by a spaceTallyItem

try something like: 

((SpaceTally new spaceTally: (Array with: TextMorph with: Point)) 
	asSortedCollection: [:a :b | a spaceForInstances > b spaceForInstances]) 

SpaceTally new systemWideSpaceTally


This class has been created from a part of SystemDictionary. It still deserves a nice
clean, such as using object instead of array having 4 slots.

sd-20 June 2003