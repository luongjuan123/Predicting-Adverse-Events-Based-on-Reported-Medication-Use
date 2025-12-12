#include <iostream>
#include <cmath>

using namespace std;

struct Node {
    int x;
    Node * next;

    Node (int x) {
        this -> x = x;
        this -> next = NULL;
    }

};

int size(Node *head) {
    int cnt = 0;
    while (head != NULL) {
        cnt ++;
        head = head->next;
    }
    return cnt;
}

Node *makeNode(int x) {
    return new Node(x);
}


void addFront(Node *&head, int x) {
    Node *tmp = makeNode(x);
    if (head == NULL) {
        head = tmp;
    }
    else {
        tmp->next = head;
        head = tmp;
    }
}

void addLast(Node *&head, int x) {
    Node *tmp = makeNode(x);
    if (head == NULL) {
        head = tmp;
    }
    else {
        Node *head1 = head;
        while (head1->next != NULL) {
            head1 = head1->next;
        }
        head1->next = tmp;
    }
}
void display(Node * head) {
    while (head != NULL) {
        cout << head -> x << " ";
        head = head -> next;
    }
}

void addMid(Node *&head, int x, int pos) {
    if (pos == 0) {
        addFront(head, x);
    }
    else if (pos = size(head) - 1) {
        addLast(head, x);
    }
    else {
        Node *tmp = head;
        for (int i = 0; i < pos - 3; i ++) {
            tmp = tmp->next;
        }
        Node *p = makeNode(x);
        p->next = tmp->next->next;
        tmp->next = p;
    }
}

void delLast(Node *&head) {
    Node *tmp = head;
    while(tmp->next->next != NULL) {
        tmp = tmp->next;
    }
    tmp->next = NULL;
}

void delFront(Node * head) {
    head = head->next;
}
 void delMid(Node *&head, int pos) {
     Node *tmp = head;
     for (int i = 0; i < pos - 3; i ++) {
         tmp = tmp->next;
     }
     tmp->next = tmp->next->next;
 }
int main() {
    
    cout << "Initial element :";

    int a; cin >> a;
    
    cout << endl;
    Node *head = makeNode(a);
    
    


    while (true) {
        int choice; 
        
        
        cout << "=========MENU=========\n";
        cout << "1. Add a element into the Front of the list !\n";
        cout << "2. Add a element into the Back of the list !\n";
        cout << "3. Add a element into the k'th position in the list!\n";
        cout << "4. Print the size and display the list !\n";
        cout << "5. Delete the last element of the list !\n";
        cout << "6. Delete the First element of the list !\n";
        cout << "7. Delete the k'th element of the list !\n";
        cout << "9. Exit\n";    
        cout << "CHOICE: ";
        cin >> choice;
        cout << endl;

        if (choice == 1) {
            int x; cin >> x;
            addFront(head, x);
        }
        else if (choice == 2) {
            int x; cin >> x;
            addLast(head, x);
        }
        else if (choice == 3) {
            int x, pos; cin >> x >> pos;
            if (pos < 0 || pos > size(head) - 1) {
                cout << "invalid position\n";
                continue;
            }
            addMid(head, x, pos);
        }
        else if (choice == 4) {
            cout << size(head) << endl;
            display(head);
            cout << endl;
        }
        else if (choice == 5) {
            delLast(head);
        }
        else if (choice == 6) {
            delFront(head);
        }
        else if (choice == 7) {
            int pos; 
            cout << "k:";
            cin >> pos;
            cout << endl;
            delMid(head, pos);
        }
        else {
            break;
        }
    }


    return 0;
}