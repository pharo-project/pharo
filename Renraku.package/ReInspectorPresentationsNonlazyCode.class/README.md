If you execute some code during the method execution it will  slow down the inspector.

Try to put all the code in blocks in the setup messages, such as #title:, #display:,  #when:. Buy doing this your code will be executed lazily when the presentation is selected.