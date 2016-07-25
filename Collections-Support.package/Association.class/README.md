I represent a pair of associated objects--a key and a value. My instances can serve as entries in a dictionary.


Implementation notes: 
Note that hash is not redefined even if the = was redefined because Association>>hash may cause extreme slowdowns in compiling Unicode methods. Association>>hash does not need to hash the value; it's slow and useless.

