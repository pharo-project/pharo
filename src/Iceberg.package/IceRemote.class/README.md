I represent a reference to an upstream repository, i.e. the repository to which you want to push/pull. Usually I am called a  'remote' but I could also be local. Most frequently each repository has at least one remote named 'origin'.

I am abstract, my subclasses provide access to different kinds of remotes.

Right now I just provide information about the different parts of the url, I could get smarter in the future.

Internal Representation and Key Implementation Points.
    Instance Variables
	url:		<String>
