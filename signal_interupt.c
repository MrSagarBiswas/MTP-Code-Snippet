#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>

void signal_handler(int Sig) {
    printf("\n\nSignal %d, Breaking...", Sig);
    exit(1);
}

int main() {
    
    signal(SIGINT, signal_handler);
    
    int i=0;
    while(1){
        printf("This Program is Running for %i Seconds\r", i);
        sleep(1);
        i++;
    }

    return 0;
}