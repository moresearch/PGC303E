#
#	Makefile
#
.POSIX:
.PHONY: all clean help 

DATALAKE		:= ./DATALAKE
DATA			:= ./DATA
MAP 			:= ./MAP
FIG 			:= ./FIG
D1				:= pacman
D2				:= space-invaders 

report: ./report.Rmd 
	@./g.R report.Rmd  

plot:
	@./plot DATA/all.csv

domains: 
	@cat DATALAKE/pacman/pacman.csv DATALAKE/space-invaders/space-invaders.csv | sort -u > DATA/all.csv

$(D1): 
	@./domain $(D1) $(D1) game c 
	@./stats $(D1) 
	@@$(D1) DONE

$(D2): 
	@./domain $(D2) $(D2) game c 
	@./stats $(D2) 
	@@$(D2) DONE

## make help: shows this message
help: Makefile
	@sed -n 's/^##//p' $<

## make clean: cleans all
clean:
	@rm -rf $(DATA)/*.*
	@rm -rf $(MAP)/*.*
	@rm -rf $(FIG)/*.*
