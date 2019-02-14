#include <stdio.h>


#include "triangle_routines.h"
#define NMOVES 36
#define BSIZE 15

int moves[NMOVES][3] =
{
  {0, 1, 3},
  {0, 2, 5},
  {1, 3, 6},
  {1, 4, 8},
  {2, 4, 7},
  {2, 5, 9},
  {3, 1, 0},
  {3, 6, 10},
  {3, 7, 12},
  {3, 4, 5},
  {4, 7, 11},
  {4, 8, 13},
  {5, 2, 0},
  {5, 9, 14},
  {5, 8, 12},
  {5, 9, 14},
  {6, 3, 1},
  {6, 7, 8},
  {7, 4, 2}, 
  {7, 8, 9},
  {8, 7, 6},
  {8, 4, 1},
  {9, 5, 2},
  {9, 8, 7},
  {10, 6, 3},
  {10, 11, 12},
  {11, 7, 4},
  {11, 12, 13},
  {12, 7, 3},
  {12, 11, 10},
  {12, 8, 5},
  {12, 13, 14},
  {13, 12, 11},
  {13, 8, 4},
  {14, 9, 5},
  {14, 13, 12},
};

/* Returns the number of pegs on the board. */
int npegs(int board[])
{
    int pegs = 0;
    int i;

    for(i = 0; i < BSIZE; i ++)
    {
        /* if board[i] = 1, increment pegs */ 
        if (board[i])
        {
            pegs ++;
        }
    }
    return pegs;

}

/* Return 1 if the move is valid on this board, otherwise return 0. */
int valid_move(int board[], int move[])
{
    int val = 0; 
    int i;
    /* check if last piece is empty and other pieces have pegs */
    if (board[move[0]] && board[move[1]] && !board[move[2]])
    {
        for (i = 0; i < NMOVES; i ++)
        {
            if (move == moves[i])
            {
                val = 1;
            }
        }
    }

    return val;

}

/* Make this move on this board if valid */
void make_move(int board[], int move[])
{
    int i; 

    /* 
     * all three postiions must be inverted
     * ones that have pegs must be removed
     * ones that don't have pegs must now have pegs
     */
    for (i = 0; i < 3; i ++)
    {
        board[move[i]] = !board[move[i]];
    }
}



/* Unmake this move on this board. */
void unmake_move(int board[], int move[])
{
  int i;
    
    /* first must check if move can be reversed */
    if (!board[move[0]] && !board[move[1]] && board[move[2]])
    {
        for (i = 0; i < 3; i ++)
        {
            board[move[i]] = !board[move[i]];
        }

    }

}

/* 
 * Solve the game starting from this board.  Return 1 if the game can
 * be solved; otherwise return 0.  Do not permanently alter the board passed
 * in. Once a solution is found, print the boards making up the solution in
 * reverse order. 
 */
int solve(int board[])
{
    int solved;
    int b;
    int m;

    /* board is solved when there is only one peg on the board */
    /* base case */

    if (npegs(board) == 1)
    {
        return 1;
    }


    /* 
     * must make all possible valid moves
     * and check if the new board is solvable
     */

    for (b = 0; b < BSIZE; b++)
    {
        for (m = 0; m < NMOVES; m++)
        {
            /* check if move is valid */
            /* first int in move must equal b */
            if (moves[m][0] == b && valid_move(board, moves[m]))
            {
                /* make the move, check new state, and then undo move */
                make_move(board, moves[m]);
                solved = solve(board);
                unmake_move(board, moves[m]);

                /* if solved, print board and return 1 */
                if(solved)
                {
                    traingle_print(board);
                    return 1;
                }
            }

        }
    }

    return 0;
}


int main(void)
{
    int board[BSIZE];
    int solved;

    traingle_input(board);
    solved = solve(board);


    /* print if board is unsolvable */
    if (!solved)
    {
        printf("Board cannot be solved\n");
    }

    return 0;

}



