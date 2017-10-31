This smell arises when instance variables are defined in all subclasses. Many times you might want to pull the instance variable up into the class so that all the subclasses do not have to define it. In addition have a look at the initialize method in each of the subclasses because if the instance variable is really the same, it will be initialized similarly in different places.

