accessing
orderedClasses
^ self package orderedClasses select:[:c | c category = self categoryName]