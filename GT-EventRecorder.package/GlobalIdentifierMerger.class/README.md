I am able to merge dictionaries of existing (in the image) and stored (on a disk) values.

First, I identify if the image values belongs to the current computer. Then I decide to favor existing or stored values. It is importatnt to notice that I change the existing dictionary received by #existing: message. GlobalIdentifierPersistence used me when he needs to load values from the local disk.

Collaborators: GlobalIdentifierPersistence 

Public API and Key Messages

- existing:
- stored: 
- merge

Example:
	GlobalIdentifierMerger new
		existing: (Dictionary newFromPairs: #(#a 1 #b  2));
		stored: (Dictionary newFromPairs: #(#a 3 #c  4));
		merge;
		existing
 
Internal Representation and Key Implementation Points.

    Instance Variables
	existing:		<Dictionary>
	stored:		<Dictionary>
