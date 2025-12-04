The Set Game


  Our project is a recreation of the game Set, where, given 12 cards in a round,
  the user must find a SET of three. For the SET to be valid, in each individual
  feature of color, shading, number, and shape, the SET must have either all of 
  the cards' features be the same or for all of them be different. Our game has 
  three modes -  the first one has a timer that keeps track of the time elapsed 
  for the whole game and the time taken to distinguish each set. This mode 
  records how long it takes to complete the entire game. The second version 
  allows the user to set a time limit in minutes on how long they want to play 
  the game. The game will then record how many sets the user managed to find 
  within the period and will record the sets per minute found in the 
  leaderboard. The third version has no timer, so nothing will be recorded. If 
  you choose to play the game in modes 1 or 2, your corresponding username and 
  score will be recorded in a leaderboard (a .txt file) which is continuously 
  updated to reflect all the people who have played the game. Users also have 
  the option to display the leaderboard. 


To run this system, simply download the project. Open the terminal, and make 
sure that you are in the project directory. Then, run dune build. In order to 
play the game, run dune exec bin/main.exe in the terminal. For instructions 
on how to play the game, run dune exec bin/main.exe -- -h in the terminal. 
From here on out, simply follow the instructions given by the program.
