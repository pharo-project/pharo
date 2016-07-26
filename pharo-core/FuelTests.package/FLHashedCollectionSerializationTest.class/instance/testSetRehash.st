tests
testSetRehash
	|  aSet person1 person2 person3 materializedSet newPerson1 |
	aSet := Set new.
	person1 := FLPerson new id: 1.
	person2 := FLPerson new id: 5.
	person3 := FLPerson new id: 8.
	aSet
		add: person1;
		add: person2;
		add: person3.

	self assert: (aSet includes: person1).
	self assert: (aSet includes: person2).
	self assert: (aSet includes: person3).
	
	materializedSet := self resultOfSerializeAndMaterialize: aSet.

    "If Fuel serializes the Set normally, it will copy its internal array just as it is. Hence, the element will be in the same order. But the objects instead may be recreated. SqueakVM assigns an identity hash at creation time (stored in the object header). So, all objects who didn't implement #hash will be finally using #identityHash. I cannot change the identityHash of an object,  so I do a test changing the id by hand.
Now, let's say that the id of person 1 change to 3 (this would be the same case when Fuel creates new objects and a new identity hash is asigned) . This 3 is not coincidence. It has to be a hash such that:
    (hash \\ aSet array size) + 1   gives you a position in the array that has a next nil in the array. In this case, the person1 is at position 2 of the array and there is a nil in position 3 and one at 5..   So (3 \\ 5) + 1 = 4.  So....Set >> scanFor:  will start to search from position 4 and since it will find a nil in 5 it will assume it has to stop searching and that the object is not present. For more details check .Set >> scanFor: "
	newPerson1 := materializedSet detect: [:each | each id = person1 id].
	newPerson1 id: 3.
	"Fuel rehashes the set at the end. This means, after having materialized the instnaces of the set. In this case, we are chanign the has of the object once it was already been materialzed, so we should simulate the rehash done by Fuel"
	materializedSet rehash.
	self assert: (materializedSet includes: newPerson1).