STACK_OUTPUT = ImageCompressor-exe
NAME = imageCompressor

all:
	stack build
	stack --local-bin-path . install
	mv $(STACK_OUTPUT) ./$(NAME)

clean:
	stack clean

fclean: clean
	$(RM) -f $(NAME)
	$(RM) -f $(STACK_OUTPUT)

re: fclean all

.PHONY : all clean fclean re
