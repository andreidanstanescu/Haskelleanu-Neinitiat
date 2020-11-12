//SegTree problem - wil update it in Haskell 
#include<bits/stdc++.h>
#define N 100005
#define ll long long
using namespace std;
int segtree[4*N],v[N],n;
ll dp[N],ras;

void build(int nod,int st,int dr)
{
    if(st>dr)
        return;
    if(st == dr) {
        segtree[nod] = st;
        return;
    }
    int fs = 2*nod, fd = 2*nod+1, mid = (st+dr) >> 1;
    build(fs,st,mid);
    build(fd,mid+1,dr);
    if(v[segtree[fs]] < v[segtree[fd]])
        segtree[nod] = segtree[fd];
    else
        segtree[nod] = segtree[fs];
    return;
}

int query(int nod,int ss, int se, int ql,int qr)
{
    if (ql <= ss && qr >= se)
        return segtree[nod];

    if (se < ql || ss > qr)
        return 0;

    int fs = 2*nod, fd = 2*nod+1, mid = (ss+se) >> 1;
    //return max(query(fs, ss, mid, ql, qr),query(fd, mid + 1, se, ql, qr));
    int stanga= query(fs, ss, mid, ql, qr) , dreapta= query(fd, mid + 1, se, ql, qr);
    if(v[stanga] < v[dreapta])
        return dreapta;
    else
        return stanga;

}


int main()
{
    ios::sync_with_stdio(false);
    cin.tie(NULL);
    cout.tie(NULL);
    cin>>n;
    for(int i=1;i<n;++i)
        cin>>v[i];
    //v[n]=0;
    build(1,1,n);
    dp[n] = 0;
    //cout << segtree[1];
    for(int i = n - 1;i >= 1; --i)
    {
        if(v[i]>=n)
            dp[i] = n - i;
        else {
            int m = query(1,1,n,i+1,v[i]);
            cerr << m <<' ';
            dp[i] = dp[m] - (v[i] - m) + (n - i);
        }
        ras += dp[i];
    }
    cout<<ras;
}

