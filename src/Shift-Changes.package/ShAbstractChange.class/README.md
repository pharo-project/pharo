I represent a change in a class detected when compared with the old version of the class.

In the changes could be only one instance of each change. So the equals and hashcode is implemented in that way.
This is because, many different change detectors can detect the same change, but I only care once. 

My  subclasses should know:

- To announce the changes if they have to.
- If they represent a change in the instance shape, requiring a  migration of instances.
- If the change affect the subclasses.