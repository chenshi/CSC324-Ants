rm -f ./src/*.hi ./src/*.o
ghc --make ./src/MyBot.hs ./src/Ants.hs
mv ./src/MyBot ./
python tools/playgame.py "./MyBot" "python tools/sample_bots/python/HunterBot.py" --map_file tools/maps/example/tutorial1.map --log_dir game_logs --turns 200 --scenario --food none --player_seed 42 --verbose -e