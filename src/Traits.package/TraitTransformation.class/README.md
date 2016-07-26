A trait transformation is an instance of one of my concrete subclasses, TraitAlias or TraitExclusion. These represent a transformation of a trait, specified by the alias and exclusion operators. 

I define an instance variable named subject which holds the object that is transformed.  Thus, an alias transformation has as its subject a trait, and a trait exclusion has as its subject either a trait alias or a trait. Each of the concrete transformation classes implement the method allSelectors according to the transformation it represents. 

(There was formerly a subclass called TraitHolder, which was the identity transformation and which did not modify the trait.  This was clearly redundant, and was removed.)