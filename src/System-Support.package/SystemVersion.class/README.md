I am responsible for maintaining what version of Pharo and the VM is running.  I also track all of the update items that have been included in the image.

I'm invoked at auto start to get the latest plugins, etc.

Some queries are
	SystemVersion current major
	SystemVersion current minor
	SystemVersion current suffix
	SystemVersion current	highestUpdate