fileIn/Out
superclassOrder: classes
    "Arrange the classes in the collection, classes, in superclass order so the 
    classes can be properly filed in. Do it in sets instead of ordered collections.
    Preserve provided classes order when there is no hierarchy relationship."

    | all list unprocessedClasses cache |
    list := classes copy asArray. "list is indexable"
    unprocessedClasses := classes asSet. 
    cache := Dictionary new.
    all := OrderedCollection new: unprocessedClasses size.
    unprocessedClasses size timesRepeat:
        [ | nextClass nextClassIndex |
            nextClassIndex := list findFirst: [:aClass | aClass notNil
                and: [(unprocessedClasses includes: aClass)
                and: [self hasNoDependenciesFor: aClass in: unprocessedClasses cache: cache]]].
            nextClass := list at: nextClassIndex.
            list at: nextClassIndex put: nil.
            all addLast: nextClass.
            unprocessedClasses remove: nextClass].
    ^all