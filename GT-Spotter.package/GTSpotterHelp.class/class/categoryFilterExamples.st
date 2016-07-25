documentation
categoryFilterExamples
	^ HelpTopic 
		title: 'Category Filter Examples'
		contents: 
'By default, Spotter searches for multiple things at the same time through search processors. Each of the processor offers the results in a separate category. For a list of all categories see the extensions help item.

While this is useful to discover different alternatives, when we know what we are looking for, we want to narrow the search for only one processor.

Let''s take an example. Suppose we want to search for packages related to Spotter. We start by searching for:
	spotter

The first item in the list is the Spotter item from the #Menu search category. This is certainly not what we are looking for. The next category shows us #Classes. This is again not what we are interested in. And then lower down the list, we see the #Packages category. That is what we are after.

One option is to navigate to that category. Another option is to scope the search to only show that category:
	spotter #packages

Now we get only the packages because # introduces a special search that matches the name of the category instead of the name of the item. In fact, to get the same result it is enough to say:
	spotter #pa

... because #Packages is the only top search category that begins with #pa.

This search scoping mechanism can be used for any category available in the current search step.

A less obvious situation happens when we have category names that contain a space. For example, in the top search we have:
- #Playground cached pages
- #Playground stashed pages

In this situation, it is possible to specify the scoping by ignoring the whitespace. For example:
	#playgroundcached
	#playgroundcachedpa
	#playgroundst
	#playgrounds'