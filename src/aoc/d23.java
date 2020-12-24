package aoc;

import java.util.HashSet;
import java.util.Set;


class Node {
    Node next;
    int value;
    Node(int value) {
        this.value = value;
    }
}

class Game {
    Node current;
    Node first;
    int size;
    int move;
    Node nodes[];

    Game(int[] values) {
        this(values, values.length);
    }

    Game(int[] values, int capacity) {
        Node first = null;
        Node last = null;
        int n = values.length;
        Node[] nodes = new Node[capacity+1];

        // Fill in given values
        for (int i=0 ; i<n ; ++i) {
            int v = values[i];
            Node node = new Node(v);
            nodes[v] = node;
            if (first == null) {
                first = node;
            }
            if (last != null) {
                last.next = node;
            }
            last = node;
        }

        // Fill to capacity
        for (int v = n+1 ; v <= capacity ; ++v) {
            Node node = new Node(v);
            nodes[v] = node;
            last.next = node;
            last = node;
        }

        // close the loop
        last.next = first;

        this.size = capacity;
        this.nodes = nodes;
        this.first = this.current = first;
        this.move = 1;
    }

    void verify() {
        for (Node n : this.nodes) {
            if (n != null) {
                System.out.println(n.value + ":" + n.next);
            }
        }
    }

    String cupsToString() {
        Node p = this.first;
        String cups = "";
        do {
            if (p != this.first) {
                cups += " ";
            }
            if (p == this.current) {
                cups += "(" + p.value + ")";
            } else {
                cups += "" + p.value;
            }
            p = p.next;
        } while (p != this.first);
        return cups;
    }

    int[] hand() {
        Node p = this.current.next;
        int[] v = new int[3];
        v[0] = p.value;
        v[1] = p.next.value;
        v[2] = p.next.next.value;
        return v;
    }

    String handToString() {
        int[] hand = this.hand();
        return String.join(" ", ""+hand[0], ""+hand[1], ""+hand[2]);
    }

    int destination() {
        Set<Integer> s = new HashSet<Integer>();
        int[] hand = this.hand();
        for (int v : hand) {
            s.add(v);
        }
        int d = this.current.value - 1;
        while (s.contains(d) || (d < 1)) {
            if (d < 1) {
                d = this.size;
            } else {
                d = d - 1;
            }
        }

        return d;
    }

    Game move() {
        Node handFirst = this.current.next;
        Node handLast  = handFirst.next.next;
        Node handAfter = handLast.next;
        Node dest      = this.nodes[this.destination()];

        // remove the hand from the circle
        this.current.next = handAfter;

        // insert the hand after destination
        handLast.next = dest.next;
        dest.next = handFirst;

        // advance current
        this.current = handAfter;

        this.move += 1;

        return this;
    }

    Game multiMove(int n) {
        for (int i = 0 ; i < n ; ++i) {
            this.move();
        }
        return this;
    }

    void show() {
        Node p = this.first;
        System.out.println("-- move " + this.move + " --");
        System.out.println("cups: " + this.cupsToString());
        System.out.println("hand: " + this.handToString());
        System.out.println("destination: " + this.destination());
        System.out.println();
    }

    String announce() {
        Node one = this.nodes[1];
        Node p = one.next;
        String s = "";
        while (p != one) {
            s += p.value;
            p = p.next;
        }
        return s;
    }

    String findStars() {
        Node one = this.nodes[1];
        long a = one.next.value;
        long b = one.next.next.value;
        return "[" + a + " "  + b + " " + (a * b) + "]";
    }
}


public class d23 {
    public static void main(String[] a){
        int example[] = {3, 8, 9, 1, 2, 5, 4, 6, 7};
        int input[] = {7, 8, 9, 4, 6, 5, 1, 2, 3};

        int M = 1000 * 1000;

        System.out.println(new Game(example).multiMove(100).announce());
        System.out.println(new Game(input).multiMove(100).announce());

        System.out.println(new Game(example, M).multiMove(10*M).findStars());
        System.out.println(new Game(input, M).multiMove(10*M).findStars());
    }
}
