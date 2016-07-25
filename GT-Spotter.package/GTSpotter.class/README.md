I am  the main model class.

I handle the list of current search steps and provide the main functionality for spawning new search steps or removing old ones.
I maintain search steps as a stack.
 
I  trigger a search when the user enters a query (#setText:from:) and maintain the search text.

To avoid crashes due to exceptions in search processors I can install an exception handler that prints exceptions to the transcript instead of opening a debugger (#beDeployment). Afternatively I can install an exception handler that  propagates exceptions (#beDeployment)