I permit to save the execution flow and to restart it later. I was originally used in seaside.

Example :

You have an object with the instance variable executionFlow.

You save the current execution flow with :
Continuation currentDo: [ :cc | executionFlow := cc]

You restart the execution flow with :
executionFlow value: true


