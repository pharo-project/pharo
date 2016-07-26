I am a dictionary holding only weakly on my keys. This is a bit dangerous since at any time my keys can go away. Clients are responsible to register my instances by WeakArray such that the appropriate actions can be taken upon loss of any keys. As key may disappear at any time, my reported size may be greater than the number of keys encountered in iterations.

See WeakRegistry for an example of use.

Implementation notes:

I am a set of WeakAssociations. Each WeakAssociation can be in one of three states: a key is present, or the key has been garbage-collected, or the association is expired (meaning that the value has also been released). During finalization, associations with no key expire, but I still keep them to avoid rehashing the entire set. When adding a new entry, the new entry can either go into a slot that is nil, or one that has an expired association. I keep a count of expired associations and rehash when there are too many (currently, if they account for more than 25% of the space).